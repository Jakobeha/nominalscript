use std::fmt::{Display, Formatter};

pub struct ValueAndCtx<'a, 'b, T: DisplayWithCtx<Ctx> + ?Sized, Ctx: ?Sized> {
    value: &'a T,
    ctx: &'b Ctx
}

pub struct ValueAndCtx2<'a, 'b, 'c, T: DisplayWithCtx2<Ctx1, Ctx2> + ?Sized, Ctx1: ?Sized, Ctx2: ?Sized> {
    value: &'a T,
    ctx: (&'b Ctx1, &'c Ctx2),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Indent(pub usize);

impl Indent {
    pub const ZERO: Self = Indent(0);

    pub fn next(&self) -> Indent {
        Indent(self.0 + 1)
    }

    pub fn prev(&self) -> Indent {
        Indent(self.0 - 1)
    }
}

impl Display for Indent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.0 {
            write!(f, "  ")?;
        }
        Ok(())
    }
}

pub trait DisplayWithCtx<Ctx: ?Sized> {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &Ctx) -> std::fmt::Result;

    fn with_ctx<'a, 'b>(&'a self, ctx: &'b Ctx) -> ValueAndCtx<'a, 'b, Self, Ctx> where Self: Sized {
        ValueAndCtx {
            value: self,
            ctx
        }
    }
}

pub trait DisplayWithCtx2<Ctx1: ?Sized, Ctx2: ?Sized> {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: (&Ctx1, &Ctx2)) -> std::fmt::Result;

    fn with_ctx<'a, 'b, 'c>(&'a self, ctx: (&'b Ctx1, &'c Ctx2)) -> ValueAndCtx2<'a, 'b, 'c, Self, Ctx1, Ctx2> where Self: Sized {
        ValueAndCtx2 {
            value: self,
            ctx
        }
    }
}

impl<T: DisplayWithCtx<Ctx>, Ctx: ?Sized> DisplayWithCtx<Ctx> for Box<T> {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &Ctx) -> std::fmt::Result {
        self.as_ref().fmt(f, ctx)
    }
}

impl<T: DisplayWithCtx2<Ctx1, Ctx2>, Ctx1: ?Sized, Ctx2: ?Sized> DisplayWithCtx2<Ctx1, Ctx2> for Box<T> {
    fn fmt(&self, f: &mut Formatter<'_>, ctxs: (&Ctx1, &Ctx2)) -> std::fmt::Result {
        self.as_ref().fmt(f, ctxs)
    }
}

// I don't really care that this is not actually used.
// Should probably move this code into a crate eventually
#[allow(dead_code)]
impl<Ctx: ?Sized> dyn DisplayWithCtx<Ctx> {
    fn with_ctx<'a, 'b>(&'a self, ctx: &'b Ctx) -> ValueAndCtx<'a, 'b, dyn DisplayWithCtx<Ctx>, Ctx> {
        ValueAndCtx {
            value: self,
            ctx
        }
    }
}

#[allow(dead_code)]
impl<Ctx1: ?Sized, Ctx2: ?Sized> dyn DisplayWithCtx2<Ctx1, Ctx2> {
    fn with_ctx<'a, 'b, 'c>(&'a self, ctx: (&'b Ctx1, &'c Ctx2)) -> ValueAndCtx2<'a, 'b, 'c, dyn DisplayWithCtx2<Ctx1, Ctx2>, Ctx1, Ctx2> {
        ValueAndCtx2 {
            value: self,
            ctx
        }
    }
}

impl<'a, 'b, T: DisplayWithCtx<Ctx> + ?Sized, Ctx: ?Sized> Display for ValueAndCtx<'a, 'b, T, Ctx> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f, &self.ctx)
    }
}

impl<'a, 'b, 'c, T: DisplayWithCtx2<Ctx1, Ctx2> + ?Sized, Ctx1: ?Sized, Ctx2: ?Sized> Display for ValueAndCtx2<'a, 'b, 'c, T, Ctx1, Ctx2> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let (ctx1, ctx2) = self.ctx;
        self.value.fmt(f, (&ctx1, &ctx2))
    }
}