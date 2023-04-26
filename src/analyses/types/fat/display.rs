use std::fmt::{Display, Formatter};
use join_lazy_fmt::Join;
use crate::analyses::types::{FnType, Nullability, Field, Optionality, OptionalType, ThinType, TypeIdent, TypeStructure, TypeTrait, TypeParam, Variance, FatType, FatTypeHole, InheritedTrait, FatTypeInherited, NominalGuard, FatRestArgType, ReturnType, RestArgTrait, TypeArgTrait, FatTypeArg};
use crate::misc::DisplayWithCtx;

/// Context which affects how a type is formatted when printed. This includes rules like
/// parentheses which affect semantics if you are displaying a type inside of another type.
#[derive(Debug, Clone, Copy, Default)]
pub struct TypeDisplayCtx {
    /// Whether functions must be parenthesized because there is are trailing symbols,
    /// and otherwise those symbols only belong to the return value
    must_paren_fn: bool
}

/// Marker to display the "inherited" datatype (e.g. `Vec<ThinType>` for thin type) in inherited position
#[derive(Debug, Clone, Copy, Default)]
pub struct DisplayInherited;

impl Display for ThinType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.with_ctx(&TypeDisplayCtx::default()))
    }
}

impl Display for FatType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.with_ctx(&TypeDisplayCtx::default()))
    }
}

impl DisplayWithCtx<TypeDisplayCtx> for ThinType {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &TypeDisplayCtx) -> std::fmt::Result {
        match self {
            ThinType::Any => write!(f, "Any"),
            ThinType::Never { nullability } => match nullability {
                Nullability::NonNullable => write!(f, "Never"),
                Nullability::Nullable => write!(f, "Null"),
            }
            ThinType::Structural {
                nullability,
                structure
            } => {
                let mut ctx = *ctx;
                if matches!(nullability, Nullability::Nullable) {
                    ctx.must_paren_fn = true;
                }
                write!(f, "{}{}", structure.with_ctx(&ctx), nullability)
            },
            ThinType::Nominal {
                nullability,
                id
            } => write!(f, "{}{}", id, nullability),
            ThinType::IllegalVoid { loc: _ } => write!(f, "Void"),
        }
    }
}

impl DisplayWithCtx<TypeDisplayCtx> for FatType {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &TypeDisplayCtx) -> std::fmt::Result {
        match self {
            FatType::Any => write!(f, "Any"),
            FatType::Never { nullability } => match nullability {
                Nullability::NonNullable => write!(f, "Never"),
                Nullability::Nullable => write!(f, "Null"),
            }
            FatType::Structural {
                nullability,
                structure
            } => {
                let mut ctx = *ctx;
                if matches!(nullability, Nullability::Nullable) {
                    ctx.must_paren_fn = true;
                }
                write!(f, "{}{}", structure.with_ctx(&ctx), nullability)
            },
            FatType::Nominal {
                nullability,
                id,
                inherited
            } => if inherited.is_empty_inherited() {
                write!(f, "{}{}", id, nullability)
            } else {
                let mut ctx = *ctx;
                if matches!(nullability, Nullability::Nullable) {
                    ctx.must_paren_fn = true;
                }
                if ctx.must_paren_fn {
                    write!(f, "(")?;
                }
                write!(f, "{} & {}", id, inherited)?;
                if ctx.must_paren_fn {
                    write!(f, ")")?;
                }
                write!(f, "{}", nullability)
            },
            FatType::Hole { nullability, hole } => write!(f, "{}{}", hole, nullability),
        }
    }
}

impl Display for FatTypeInherited {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.with_ctx(&DisplayInherited))
    }
}

impl DisplayWithCtx<DisplayInherited> for FatTypeInherited {
    fn fmt(&self, f: &mut Formatter<'_>, &DisplayInherited: &DisplayInherited) -> std::fmt::Result {
        let mut wrote = false;
        if self.is_never {
            wrote = true;
            write!(f, "Never")?;
        }
        for super_id in &self.super_ids {
            if wrote {
                write!(f, " & ")?;
            }
            wrote = true;
            write!(f, "{}", super_id)?;
        }
        if let Some(structure) = self.structure.as_ref() {
            if wrote {
                write!(f, " & ")?;
            }
            wrote = true;
            write!(f, "{}", structure)?;
        }
        for typescript_type in &self.typescript_types {
            if wrote {
                write!(f, " & ")?;
            }
            wrote = true;
            write!(f, "#ts({})", typescript_type)?;
        }
        for guard in &self.guards {
            if wrote {
                write!(f, " & ")?;
            }
            wrote = true;
            write!(f, "{}", guard)?;
        }
        Ok(())
    }
}

impl Display for NominalGuard {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "guard ({}) {}", self.param, self.body)
    }
}

impl Display for FatTypeArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{{}}} {}", self.variance_bound, self.type_)
    }
}

impl Display for FatRestArgType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FatRestArgType::None => Ok(()),
            FatRestArgType::Array { element } => write!(f, "{}[]", element.with_ctx(&TypeDisplayCtx { must_paren_fn: true })),
            FatRestArgType::Illegal { intended_type } => write!(f, "{}", intended_type)
        }
    }
}

impl Display for FatTypeHole {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "#[{:p}]({})", self.upper_bound, self.upper_bound.borrow())
    }
}

impl Display for Nullability {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Nullability::NonNullable => Ok(()),
            Nullability::Nullable => write!(f, "?"),
        }
    }
}

impl Display for Optionality {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Optionality::Required => Ok(()),
            Optionality::Optional => write!(f, "?"),
        }
    }
}

impl<Type: TypeTrait<TypeArg=TypeArg> + Display, TypeArg: TypeArgTrait + Display> Display for TypeIdent<Type> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.generic_args.is_empty() {
            write!(f, "<{}>", ", ".join(&self.generic_args))?;
        }
        Ok(())
    }
}

impl<Type: TypeTrait<Inherited=Inherited, TypeArg=TypeArg, RestArg=RestArgType> + DisplayWithCtx<TypeDisplayCtx> + Display, Inherited: InheritedTrait + DisplayWithCtx<DisplayInherited>, TypeArg: TypeArgTrait + Display, RestArgType: RestArgTrait + Display> Display for TypeStructure<Type> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.with_ctx(&TypeDisplayCtx::default()))
    }
}

impl<Type: TypeTrait<Inherited=Inherited, TypeArg=TypeArg, RestArg=RestArgType> + DisplayWithCtx<TypeDisplayCtx> + Display, Inherited: InheritedTrait + DisplayWithCtx<DisplayInherited>, TypeArg: TypeArgTrait + Display, RestArgType: RestArgTrait + Display> DisplayWithCtx<TypeDisplayCtx> for TypeStructure<Type> {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &TypeDisplayCtx) -> std::fmt::Result {
        match self {
            TypeStructure::Fn { fn_type } => write!(f, "{}", fn_type.with_ctx(ctx)),
            TypeStructure::Array { element_type } => {
                let mut ctx = *ctx;
                ctx.must_paren_fn = true;
                write!(f, "{}[]", element_type.with_ctx(&ctx))
            },
            TypeStructure::Tuple { element_types } => write!(f, "[{}]", ", ".join(element_types)),
            TypeStructure::Object { field_types } => write!(f, "{{ {} }}", ", ".join(field_types)),
        }
    }
}

impl<Type: TypeTrait<Inherited=Inherited, TypeArg=TypeArg, RestArg=RestArgType> + Display, Inherited: InheritedTrait + DisplayWithCtx<DisplayInherited>, RestArgType: RestArgTrait + Display, TypeArg: TypeArgTrait + Display> DisplayWithCtx<TypeDisplayCtx> for FnType<Type> {
    fn fmt(&self, f: &mut Formatter<'_>, ctx: &TypeDisplayCtx) -> std::fmt::Result {
        if ctx.must_paren_fn {
            write!(f, "(")?;
        }
        if !self.type_params.is_empty() {
            write!(f, "<{}> ", ", ".join(&self.type_params))?;
        }
        write!(f, "(")?;
        let mut wrote_args = false;
        if !self.this_type.is_any() {
            write!(f, "this: {}", self.this_type)?;
            wrote_args = true;
        }
        if !self.arg_types.is_empty() {
            if !wrote_args {
                write!(f, ", ")?;
            }
            wrote_args = true;
            write!(f, "{}", ", ".join(&self.arg_types))?;
        }
        if !self.rest_arg_type.is_empty_rest_arg() {
            if !wrote_args {
                write!(f, ", ")?;
            }
            // wrote_args = true;
            write!(f, "...{}", self.rest_arg_type)?;
        }
        write!(f, ") => {}", self.return_type)?;
        if ctx.must_paren_fn {
            write!(f, "(")?;
        }
        Ok(())
    }
}

impl<Type: TypeTrait<Inherited=Inherited, TypeArg=TypeArg> + Display, Inherited: InheritedTrait + DisplayWithCtx<DisplayInherited>, TypeArg: TypeArgTrait + Display> Display for TypeParam<Type> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.variance_bound != Variance::default() {
            write!(f, "{} ", self.variance_bound)?;
        }
        write!(f, "{}", self.name)?;
        if !self.supers.is_empty_inherited() {
            write!(f, ": {}", self.supers.with_ctx(&DisplayInherited))?;
        }
        Ok(())
    }
}

impl DisplayWithCtx<DisplayInherited> for Vec<ThinType> {
    fn fmt(&self, f: &mut Formatter<'_>, &DisplayInherited: &DisplayInherited) -> std::fmt::Result {
        write!(f, "{}", " & ".join(self))
    }
}

impl Display for Variance {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Variance::Bivariant => "biv",
            Variance::Covariant => "cov",
            Variance::Contravariant => "con",
            Variance::Invariant => "inv"
        })
    }
}

impl<Type: Display> Display for OptionalType<Type> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.optionality, self.type_)
    }
}

impl<Type: Display> Display for Field<OptionalType<Type>> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}: {}", self.name, self.type_.optionality, self.type_.type_)
    }
}

impl<Type: Display> Display for ReturnType<Type> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReturnType::Void => write!(f, "Void"),
            ReturnType::Type(type_) => write!(f, "{}", type_),
        }
    }
}