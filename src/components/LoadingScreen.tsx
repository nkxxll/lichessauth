import React, { useState, useEffect } from 'react';

// The new LoadingScreen component
const LoadingScreen = () => {
  return (
    <div className="flex flex-col items-center justify-center min-h-screen p-6 font-sans text-slate-200 bg-gradient-to-br from-blue-950 to-blue-900 transition-opacity duration-500">
      <div className="text-center">
        <h1 className="text-3xl sm:text-4xl font-bold tracking-tight mb-4 text-white">
          Loading...
        </h1>
        <p className="text-sm text-slate-400">
          Please wait while we prepare your experience.
        </p>
      </div>
    </div>
  );
};

// The LandingPage component remains unchanged
const LandingPage = () => {
  return (
    <div className="flex flex-col items-center justify-center min-h-screen p-6 font-sans text-slate-200 bg-gradient-to-br from-blue-950 to-blue-900">
      <div className="w-full max-w-2xl text-center p-8 sm:p-12">
        <div className="mb-8">
          <h1 className="text-4xl sm:text-5xl font-extrabold tracking-tight mb-4 text-white">
            Welcome to the App
          </h1>
          <p className="text-lg text-slate-400 max-w-prose mx-auto">
            Your destination for powerful game analysis and insights. Dive into your matches,
            learn from your mistakes, and master your favorite games.
          </p>
        </div>

        <div className="space-y-4">
          <button
            onClick={() => {
              window.location.href = '/api/login';
            }}
            className="w-full sm:w-auto px-8 py-4 rounded-full bg-blue-600 text-white font-bold text-lg hover:bg-blue-700 transition-colors duration-200 shadow-lg transform hover:scale-105"
          >
            Get Started
          </button>
        </div>

        <div className="mt-8 text-sm text-slate-500">
          <p>Already a member? Sign in below.</p>
          <a
            href="/api/login"
            className="text-blue-400 hover:text-blue-300 underline font-medium transition-colors duration-200"
          >
            Go to Login
          </a>
        </div>
      </div>
    </div>
  );
};

// The App component now manages the loading state
const Loading = () => {
  const [isLoading, setIsLoading] = useState(true);

  // Simulate a loading delay
  useEffect(() => {
    const timer = setTimeout(() => {
      setIsLoading(false);
    }, 2000); // 2-second delay
    return () => clearTimeout(timer); // Cleanup the timer
  }, []);

  return (
    <div className="bg-slate-950 min-h-screen font-sans overflow-y-auto scrollbar">
      {isLoading ? <LoadingScreen /> : <LandingPage />}
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

export default Loading;
