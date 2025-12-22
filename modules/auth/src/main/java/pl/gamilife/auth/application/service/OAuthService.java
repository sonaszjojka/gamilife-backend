package pl.gamilife.auth.application.service;

import pl.gamilife.auth.application.dto.GoogleSignInResult;
import pl.gamilife.auth.application.dto.GoogleUserDto;

import java.util.Map;
import java.util.UUID;

public interface OAuthService {

    Map<String, String> exchangeCodeForTokens(String code, String codeVerifier);

    GoogleUserDto extractUserInfoFromIdToken(String idToken);

    GoogleSignInResult loginViaGoogle(UUID userId, String googleEmail);

    GoogleSignInResult registerViaGoogle(GoogleUserDto googleUserDto);

}
