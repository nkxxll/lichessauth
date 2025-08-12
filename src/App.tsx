import { useEffect } from "react";
import toast, { Toaster } from "react-hot-toast";
import { useQuery, useQueryClient } from "@tanstack/react-query";
import { fetchGameList } from "./api/api";

function App() {
  function SearchQuery() {
    const params = new URLSearchParams(window.location.search);
    const logged = params.get("loggedin");
    if (logged === "true") {
      toast.success("Logged in");
    }
  }

  const queryClient = useQueryClient();

  const { data, isLoading, error } = useQuery({
    queryKey: ["gameList"],
    queryFn: fetchGameList,
  });

  function refreshGameList() {
    queryClient.invalidateQueries({ queryKey: ["gameList"] });
  }

  if (isLoading) return <p>Loading games...</p>;
  if (error instanceof Error)
    return (
      <p>
        <a href="/api/login">might have to log in</a>
        <span>{error.message}</span>
      </p>
    );

  return (
    <>
      <Toaster />
      <ul>
        {data?.map((game) => (
          <>
            <li key={game.id}>
              {game.id} ({game.moves.length} moves)
              {game.opening}
            </li>
            <button onClick={() => alert("think about this")}>
              Opening Error
            </button>
          </>
        ))}
      </ul>
      <button onClick={refreshGameList}>Refresh</button>
    </>
  );
}

export default App;
