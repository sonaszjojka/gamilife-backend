package edu.pjwstk.auth.services;

import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.GoogleLoginDTO;
import edu.pjwstk.auth.dto.service.LinkOAuthAccountDto;
import edu.pjwstk.auth.dto.service.OAuthCodeDto;

import java.util.Optional;

public interface OAuth2Service {

    Optional<AuthTokens> linkNewOAuthAccount(LinkOAuthAccountDto linkOAuthAccountDto);

    GoogleLoginDTO handleGoogleLogin(OAuthCodeDto oAuthCodeDto);

}
