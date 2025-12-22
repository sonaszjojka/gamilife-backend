package pl.gamilife.auth.application.service;

import pl.gamilife.auth.application.dto.GoogleSignInResult;
import pl.gamilife.auth.application.dto.GoogleUserDto;

import java.util.UUID;

public interface OAuthService {

    GoogleUserDto exchangeCodeForTokens(String code, String codeVerifier);

    GoogleSignInResult loginViaGoogle(UUID userId, String googleEmail);

    GoogleSignInResult registerViaGoogle(GoogleUserDto googleUserDto);

}
