import { Handle, Position } from 'reactflow';

interface TextNodeData {
    data: {
        content: string;
        onContentChange: (x: string) => void;
    };
}

// It is time to decide on the architectural direction for this application.
// I am not sure whether I want to pass in closures through the data field in order to signal the content changes.

// With Fable, we had the Elmish library and could send messages to the update loop.
// The equivalent of that would be to use reducers.

// React Flow uses Zustand under the hood, so I am leaning towards that as well.

interface TextNodeT {
    type: "Text"
    data: TextNodeData
}

interface CompilationNodeData {
    data: Record<string, never>
}

interface CompilationNodeT {
    type: "Compilation"
    data: CompilationNodeData
}

interface CompilationOutputNodeData {
    data: Record<string, never>
}

interface CompilationOutputNodeT {
    type: "CompilationOutput"
    data: CompilationOutputNodeData
}

type Nodes =
    | TextNodeT
    | CompilationNodeT
    | CompilationOutputNodeT

export function TextNode({ data }: TextNodeData) {
    // We'll add more target handles later.
    return (
        <div className="grid w-96 h-96 p-8 card bg-base-300 rounded-box place-items-center">
            <textarea className="textarea w-full h-full textarea-bordered resize-none nodrag"
                placeholder="Text"
                value={data.content}
                onChange={x => { data.onContentChange(x.target.value); }}
            ></textarea>
            <Handle type="target"
                position={Position.Top}
                className="w-8 h-8 rounded-none bg-teal-500"
            />
            <Handle type="source"
                position={Position.Bottom}
                className="w-8 h-8 rounded-none bg-teal-500"
            />
        </div>
    );
}

export function CompilationNode({ data }: CompilationNodeData) {
    return (
        <div className="grid w-fit h-fit p-8 card bg-base-300 rounded-box place-items-center">
            <button className="btn btn-lg">Compile</button>
            <Handle type="target"
                position={Position.Top}
                className="w-8 h-8 rounded-none bg-red-500"
            />
        </div>
    );
}

export function CompilationOutputNode() {
    const ar: JSX.Element[] = []
    for (let i = 0; i < 30; i++) {
        ar.push(<p key={i}>If a dog chews shoes whose shoes does he choose?</p>)
    }
    return (
        <div className="card w-fit h-fit bg-base-100 border-8">
            <div className="card-body">
                <div className="flex flex-row justify-between">
                    <h2 className="card-title">Compilation Output</h2>
                    <div className="card-actions">
                        <button className="btn btn-primary">Close</button>
                    </div>
                </div>
                {ar}
            </div>
        </div>
    );
}