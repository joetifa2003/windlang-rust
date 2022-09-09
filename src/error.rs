macro_rules! wind_error {
    ($name:ident; $($kind:ident),*) => {
        paste::paste! {
            #[derive(Debug)]
            pub enum [< $name Kind >] {
                $($kind,)*
            }

            #[derive(Debug)]
            pub struct $name {
                kind: [< $name Kind >],
                msg: String,
            }

            impl $name {
                pub fn new(kind: [< $name Kind >], msg: String) -> $name {
                    $name { kind, msg }
                }
            }

            impl ::std::fmt::Display for $name {
                fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
                    write!(
                        f,
                        "[{} {}]: {}",
                        stringify!($name),
                        match self.kind {
                            $([< $name Kind >]::$kind => stringify!($kind),)*
                        },
                        self.msg
                    )
                }
            }

            impl ::std::error::Error for $name {}
        }
    };
}

pub(crate) use wind_error;
