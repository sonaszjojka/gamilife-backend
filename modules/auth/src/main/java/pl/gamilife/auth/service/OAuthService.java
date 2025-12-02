package pl.gamilife.auth.service;

import pl.gamilife.auth.dto.GoogleUserDto;
import pl.gamilife.auth.usecase.googlesignin.GoogleSignInResult;

import java.util.Map;
import java.util.UUID;

public interface OAuthService {

    Map<String, String> exchangeCodeForTokens(String code, String codeVerifier);

    GoogleUserDto extractUserInfoFromIdToken(String idToken);

    GoogleSignInResult loginViaGoogle(UUID userId, String googleEmail);

    GoogleSignInResult registerViaGoogle(GoogleUserDto googleUserDto);

}
