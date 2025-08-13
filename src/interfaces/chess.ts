export interface Game {
  id: string;
  moves: string;
  opening: string;
  players: Array<{ user: string; id: string }>;
}

export type OcamlArrowColor = "Red" | "Green";
export type OcamlSide = "White" | "Black";

export type OcamlArrow = {
  start_square: string;
  end_square: string;
  color: [OcamlArrowColor]; // tuple with one element
};

export type OcamlChessboardOptions = {
  fen: string;
  bad_move: OcamlArrow;
  good_move: OcamlArrow;
  orientation: [OcamlSide]; // tuple with one element
};
