const LandingPage = () => {
  return (
    // The main container now has the gradient and fills the whole screen
    <div className="flex flex-col items-center justify-center min-h-screen p-6 font-sans text-slate-200 bg-gradient-to-br from-blue-950 to-blue-900">
      {/* The content container is now transparent and serves only to center the text */}
      <div className="w-full max-w-2xl text-center p-8 sm:p-12">
        <div className="mb-8">
          <h1 className="text-4xl sm:text-5xl font-extrabold tracking-tight mb-4 text-white">
            Welcome to the Opening Error Finder
          </h1>
          <p className="text-lg text-slate-400 max-w-prose mx-auto">
            Your destination for powerful game analysis and insights :D. Dive into your matches,
            learn from your mistakes, and master your favorite games.
          </p>
        </div>

        <div className="space-y-4">
          <button
            onClick={() => {
              // In a real app, you would navigate to the login page
              // or trigger a login action here.
              window.location.href = '/api/login';
            }}
            className="w-full sm:w-auto px-8 py-4 rounded-full bg-blue-600 text-white font-bold text-lg hover:bg-blue-700 transition-colors duration-200 shadow-lg transform hover:scale-105"
          >
            Get Started
          </button>
        </div>
      </div>
    </div>
  );
};

// Main application component to demonstrate the new component
const Landing = () => {
  return (
    <div className="bg-slate-950 min-h-screen font-sans overflow-y-auto scrollbar">
      <LandingPage />
      <style>
        {`
        .scrollbar::-webkit-scrollbar {
          width: 10px;
          height: 10px;
        }

        .scrollbar::-webkit-scrollbar-track {
          border-radius: 8px;
          background: #0f172a;
        }

        .scrollbar::-webkit-scrollbar-thumb {
          background: #334155;
          border-radius: 8px;
          border: none;
        }

        .scrollbar::-webkit-scrollbar-thumb:hover {
          background: #475569;
        }
        `}
      </style>
    </div>
  );
};

export default Landing;
