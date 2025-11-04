package edu.pjwstk.auth.service;

import edu.pjwstk.auth.dto.service.GoogleUserDto;
import edu.pjwstk.auth.usecase.result.GoogleLoginResult;

import java.util.Map;
import java.util.UUID;

public interface OAuthService {

    Map<String, String> exchangeCodeForTokens(String code, String codeVerifier);

    GoogleUserDto extractUserInfoFromIdToken(String idToken);

    GoogleLoginResult loginViaGoogle(UUID userId, String googleEmail);

    GoogleLoginResult registerViaGoogle(GoogleUserDto googleUserDto);

}
