package edu.pjwstk.auth.util;

import edu.pjwstk.auth.dto.service.GoogleUserDto;

import java.util.Map;

public interface OAuth2CodeUtil {

    Map<String, String> exchangeCodeForTokens(String code, String codeVerifier);

    GoogleUserDto extractUserInfoFromIdToken(String idToken);

}
