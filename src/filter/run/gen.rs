use std::future::Future;

use genawaiter::{stack::{Co, Gen, Shelf}, GeneratorState};

use ouroboros::self_referencing;

use crate::{filter::Filter, json::Json};

use super::{run, RunCtx, RunEnd};

pub type RunOut<'a> = Co<'a, Json>;

type RunGenFuture = std::pin::Pin<Box<dyn Future<Output = RunEnd> + 'static>>;

#[self_referencing]
struct InternalRunGen<'a> {
    shelf: Shelf<Json, (), RunGenFuture>,

    ctx: &'a RunCtx,
    filter: &'a Filter,
    json: &'a Json,

    #[borrows(mut shelf, ctx, filter, json)]
    #[not_covariant]
    gen: Gen<'this, Json, (), RunGenFuture>,
}
pub struct RunGen<'a> {
    inner: InternalRunGen<'a>,
    end: RunEnd,
}
impl<'a> RunGen<'a> {
    pub fn build(ctx: &'a RunCtx, filter: &'a Filter, json: &'a Json) -> Self {
        unsafe fn to_static<'a, F: Future<Output = RunEnd> + 'a>(
            fut: F,
        ) -> RunGenFuture {
            std::mem::transmute::<
                std::pin::Pin<Box<dyn Future<Output = RunEnd> + 'a>>,
                RunGenFuture,
            >(Box::pin(fut))
        }

        Self {
            inner: InternalRunGenBuilder {
                shelf: Shelf::new(),
                ctx,
                filter,
                json,
                // SAFETY: The `run` function produces a future that captures references
                // with lifetimes 'a (from ctx, filter, json) and 'this (from the `co` object).
                // Since 'a outlives 'this, the resulting future is effectively `Future + 'this`.
                // We transmute this future to `Future + 'static`. This is safe because:
                // 1. `InternalRunGen<'a>` (which lives for `'this`) owns both the `shelf`
                //    and the `gen`. The `gen` cannot outlive `shelf` or the `'a` data.
                // 2. The generator can only be polled as long as `InternalRunGen` is alive.
                //    During this time, all captured references ('a and 'this) are valid.
                // 3. `ouroboros` ensures the structural integrity and necessary drop order.
                // Thus, the "lie" of 'static is upheld for the actual duration the future is used.
                gen_builder: |shelf, ctx, filter, json| unsafe {
                    Gen::new(shelf, |co| to_static(run(co, ctx, filter, json)))
                },
            }
            .build(),
            end: None,
        }
    }

    pub fn resume(&mut self) -> GeneratorState<Json, RunEnd> {
        self.inner.with_gen_mut(|gen| gen.resume())
    }

    pub fn end(&mut self) -> RunEnd {
        self.end.take()
    }
}
impl Iterator for RunGen<'_> {
    type Item = Json;

    fn next(&mut self) -> Option<Self::Item> {
        match self.resume() {
            GeneratorState::Yielded(val) => Some(val),
            GeneratorState::Complete(end) => {
                self.end = end;
                None
            }
        }
    }
}

macro_rules! yield_ {
    ($out:ident, $val:expr) => {
        $out.yield_($val).await
    };
}
pub(crate) use yield_;
