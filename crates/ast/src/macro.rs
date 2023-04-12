#[macro_export]
macro_rules! ast_node {
    (pub enum AstNode{ $($tag:ident($ty:ty),)* }) => {
        pub enum AstNode{
            $($tag($ty),)*
        }

        $(
        impl Node for $ty{
            fn from_node(node: &AstNode) -> &$ty{
                if let AstNode::$tag(ref node) = node {
                    node
                }else{
                    panic!("invalid node id")
                }
            }

            fn from_node_mut(node: &mut AstNode) -> &mut $ty{
                if let AstNode::$tag(ref mut node) = node {
                    node
                }else{
                    panic!("invalid node id")
                }
            }

            fn into_node(self) -> AstNode{
                AstNode::$tag(self)
            }
        }
        )*

    };
}
