use super::*;

impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> PResult<'a, Stmt<'a>> {
        if is!(self, "{") {
            return Ok(Stmt::Block(self.parse_block_stmt()?));
        }
        if is!(self, "var") {
            return Ok(Stmt::Block(self.parse_decl()?));
        }
        if is!(self, ";") {
            return Ok(Stmt::Empty);
        }
        to_do!(self)
    }

    pub fn parse_block_stmt(&mut self) -> PResult<'a, Block<'a>> {
        to_do!(self)
    }

    pub fn parse_decl(&mut self) -> PResult<'a, Block<'a>> {
        if eat!(self, "var") {
            to_do!(self)
        }
        to_do!(self)
    }
}
