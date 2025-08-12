import type { Game } from "../interfaces/chess";

export async function fetchGameList(): Promise<Game[]> {
  const res = await fetch("/api/gamelist");

  if (!res.ok) {
    throw new Error(`Failed to fetch games: ${res.status}`);
  }

  return res.json();
}
