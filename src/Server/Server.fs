module Server

open Saturn
open Giraffe
open Microsoft.AspNetCore.Authentication.JwtBearer

let webApp =
    let authenticated =
        warbler (fun _ -> requiresAuthentication (challenge JwtBearerDefaults.AuthenticationScheme))

    choose [ Api.create Api.guestApi; authenticated >=> Api.create Api.userApi ]

let app = application {
    use_router webApp
    use_jwt_authentication Authorize.secret Authorize.issuer
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    run app
    0