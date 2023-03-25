pub trait SetPosition<S, P> {
    fn set_square_to_piece(&mut self, square: S, piece: P);
    fn remove_from_square(&mut self, square: S);
    fn square_piece(&self, square: S) -> P;
}
