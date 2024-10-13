module Authorize

open System
open System.IO
open System.Security.Claims
open Microsoft.IdentityModel.JsonWebTokens
open Shared

let private algorithm = Microsoft.IdentityModel.Tokens.SecurityAlgorithms.HmacSha256

let private createSecret () =
    let crypto = System.Security.Cryptography.RandomNumberGenerator.Create()
    let randomNumber = Array.init 32 byte
    crypto.GetBytes(randomNumber)
    randomNumber

let secret =
    let fi = FileInfo("./temp/token.txt")

    if not fi.Exists then
        let newSecret = createSecret ()

        if not fi.Directory.Exists then
            fi.Directory.Create()

        File.WriteAllBytes(fi.FullName, newSecret)

    File.ReadAllBytes(fi.FullName) |> System.Text.Encoding.UTF8.GetString

let issuer = "fogentroleplay.io"

let generateToken username =
    [
        Claim(JwtRegisteredClaimNames.Sub, username)
        Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString())
    ]
    |> Saturn.Auth.generateJWT (secret, algorithm) issuer (DateTime.UtcNow.AddHours(1.0))

let createUserData (login: Login) : UserData = {
    username = login.username
    token = generateToken login.username
}

let login (login: Login) = async {
    if LiteDBDatabase.isValidUserLogin login then
        return LoggedIn(createUserData login)
    else
        return UsernameOrPasswordIncorrect
}