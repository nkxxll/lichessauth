import toast, { Toaster } from "react-hot-toast";
import { useQuery, useQueryClient } from "@tanstack/react-query";
import { fetchGameList, useOpeningErrorMutation } from "./api/api";
import { Opening, type OpeningProps } from "./components/Opening";
import { useEffect, useState } from "react";

const PLAYER = "brandtheon";

function App() {
  // has to be at the top
  useEffect(() => {
    SearchQuery();
  }, []);

  const openingErrorMutation = useOpeningErrorMutation();

  function SearchQuery() {
    const params = new URLSearchParams(window.location.search);
    const logged = params.get("loggedin");
    if (logged === "true") {
      toast.success("Logged in");
    }
  }

  const [config, setConfig] = useState<OpeningProps | null>(null);

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
      <div className="flex flex-row bg-gray-700 text-white">
        <div className="w-1/2">
          <ul>
            {data?.map((game) => {
              const color = game.players[0].id === PLAYER ? "white" : "black";
              const style =
                color === "white"
                  ? "text-black bg-white"
                  : "text-white bg-black";
              return (
                <div key={game.id}>
                  <li>
                    {game.id} ({game.moves.length} moves)
                    <br />
                    <span className={`${style} m-2`}>{color}</span>
                  </li>
                  <button
                    className="p-2 m-2 border-2 rounded"
                    onClick={() => {
                      openingErrorMutation.mutate(
                        { color, moves: game.moves },
                        {
                          onSuccess: (data) => {
                            setConfig(data);
                          },
                        },
                      );
                    }}
                  >
                    Opening Error
                  </button>
                </div>
              );
            })}
          </ul>
        </div>
        <div className="w-1/2 text-black bg-gray-100">
          {config ? <Opening {...config} /> : "Choose a game to analyse"}
        </div>
      </div>
      <button onClick={refreshGameList}>Refresh</button>
    </>
  );
}

export default App;
