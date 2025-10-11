package edu.pjwstk.auth.services;

import edu.pjwstk.auth.dto.service.GoogleUserDto;

import java.util.Map;

public interface OAuth2CodeService {

    Map<String, String> exchangeCodeForTokens(String code, String codeVerifier);

    GoogleUserDto extractUserInfoFromIdToken(String idToken);

}
