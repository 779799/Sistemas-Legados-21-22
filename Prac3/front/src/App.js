import './App.css';
import './SearchBar'
import SearchBar from './SearchBar';
import { useState, useEffect } from 'react';
import axios from 'axios';

function App() {
    
  return (
      <div className="App">
        <SearchBar/>
    </div>
  );
}

export default App;
