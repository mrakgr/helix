using Helix;
using Helix.Types;
using Microsoft.AspNetCore.Components.Web;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using MudBlazor.Services;

var builder = WebAssemblyHostBuilder.CreateDefault(args);
builder.RootComponents.Add<App>("#app");
builder.RootComponents.Add<HeadOutlet>("head::after");

builder.Services.AddScoped(sp => new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });
builder.Services.AddScoped<HelixDiagram>();
builder.Services.AddScoped<Data.MediaPool>();
builder.Services.AddScoped<Propagator>();
builder.Services.AddMudServices();

var app = builder.Build();
await app.RunAsync();