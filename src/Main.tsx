import React from 'react'
import ReactDOM from 'react-dom/client'

import Flow from "./App.tsx";
import './Tailwind.css'
import { ReactFlowProvider } from 'reactflow';

ReactDOM.createRoot(document.getElementById('root') as HTMLElement).render(
    <React.StrictMode>
        <ReactFlowProvider>
            <div style={{ width: '100vw', height: '100vh' }}>
                <Flow />
            </div>
        </ReactFlowProvider>
    </React.StrictMode>,
)
