open System
open System.IO
open System.IO.Compression

let f () =

    let cwd = Directory.GetCurrentDirectory()
    use stream = new FileStream(Path.Combine(cwd,"example.zip"),FileMode.Create)
    use archive = new ZipArchive(stream,ZipArchiveMode.Update)
    let entry = archive.CreateEntry("text/readme.txt")
    use writer = new StreamWriter(entry.Open())

    writer.WriteLine("Information about this package.");
    writer.WriteLine("========================")
    
f()