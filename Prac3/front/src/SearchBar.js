import './App.css'
import React, { useState, useEffect } from 'react';
import axios from 'axios';


const SearchBar = () => {
    const NAME = "name";
    const [programs, setPrograms] = useState([]);
    const [searchWord, setSearchWord] = useState("");
    const [searchOption, setSearchOption] = useState(NAME);
    const [numReg, setNumReg] = useState("0");

    useEffect(() => {
        getNum();
    }, []);

    async function getNum() {
        await axios.get('http://192.168.1.140:8080/numRegister')
            .then(response => {
                setNumReg(response.data);
            });
    }

    async function searchByName (name) {
        await axios.get('http://192.168.1.140:8080/searchByName/' + name)
            .then(response => {
                setPrograms(response.data);
            });
    }

    async function searchByTape(tape) {
        await axios.get('http://192.168.1.140:8080/searchByTape/' + tape)
            .then(response => {
                setPrograms(response.data);
            });
    }

    async function search() {
        if (searchOption == NAME)
            await searchByName(searchWord);
        else
           await searchByTape(searchWord);
    }

    return (
        <div className="container">
            <h3>Número de registros: {numReg}</h3>

                <div className="searchContainer" >
                    <input
                        className="searchbar"
                        type="text"
                        id="header-search"
                        placeholder="Buscar"
                    value={searchWord}
                    onChange={(word) => setSearchWord(word.target.value)}
                />
                <select className="select" id="searchType" value={searchOption} onChange={(option) => setSearchOption(option.target.value)}>
                    <option value="name">Por nombre</option>
                    <option value="tape">Por cinta</option>
                </select>
                   
                <button className="searchButton" type="submit" onClick={()=>search()}>Buscar</button>
            </div>
            { programs.length>0?
                <div className="listContainer">
                    <table className="list">
                        <thead>
                            <tr>
                                <th>Registro</th>
                                <th>Nombre</th>
                                <th>Tipo</th>
                                <th>Cinta</th>
                            </tr>
                        </thead>
                        <tbody>
                        {programs && programs.map(program =>
                            <tr key={program.register}>
                                <td>{program.register}</td>
                                <td>{program.name}</td>
                                <td>{program.type}</td>
                                <td>{program.tape}</td>
                            </tr>
                            )}
                        </tbody>
                    </table>
                </div>
            :null}
            </div>
    );
}
  

export default SearchBar;
