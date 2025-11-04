package edu.pjwstk.auth.service;

import edu.pjwstk.auth.dto.service.GoogleUserDto;

import java.util.Map;

public interface OAuthService {

    Map<String, String> exchangeCodeForTokens(String code, String codeVerifier);

    GoogleUserDto extractUserInfoFromIdToken(String idToken);

}
