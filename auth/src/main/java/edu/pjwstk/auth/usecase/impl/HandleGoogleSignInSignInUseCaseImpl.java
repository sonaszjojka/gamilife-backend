package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.domain.UserOAuthProvider;
import edu.pjwstk.auth.dto.service.GoogleLoginDTO;
import edu.pjwstk.auth.dto.service.GoogleUserDto;
import edu.pjwstk.auth.dto.service.OAuthCodeDto;
import edu.pjwstk.auth.persistence.repository.UserProviderRepository;
import edu.pjwstk.auth.usecase.HandleGoogleSignInUseCase;
import edu.pjwstk.auth.usecase.LoginViaGoogleUseCase;
import edu.pjwstk.auth.usecase.RegisterViaGoogleUseCase;
import edu.pjwstk.auth.util.OAuth2CodeUtil;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Map;
import java.util.Optional;

@Service
@AllArgsConstructor
public class HandleGoogleSignInSignInUseCaseImpl implements HandleGoogleSignInUseCase {

    private final OAuth2CodeUtil oAuth2CodeUtil;
    private final UserProviderRepository userProviderRepository;
    private final UserApi userApi;
    private final LoginViaGoogleUseCase loginViaGoogleUseCase;
    private final RegisterViaGoogleUseCase registerViaGoogleUseCase;

    @Override
    @Transactional
    public GoogleLoginDTO execute(OAuthCodeDto oAuthCodeDto) {
        Map<String, String> tokenResponse = oAuth2CodeUtil.exchangeCodeForTokens(oAuthCodeDto.code(), oAuthCodeDto.codeVerifier());
        GoogleUserDto googleUserDto = oAuth2CodeUtil.extractUserInfoFromIdToken(tokenResponse.get("id_token"));

        Optional<UserOAuthProvider> existingOAuthUser = userProviderRepository
                .getOAuthUserByProviderAndProviderId("google", googleUserDto.sub());

        if (existingOAuthUser.isPresent()) {
            return loginViaGoogleUseCase.execute(existingOAuthUser.get().userId(), googleUserDto.email());
        }

        Optional<BasicUserInfoApiDto> user = userApi.getUserByEmail(googleUserDto.email());
        if (user.isPresent()) {
            // User exists, but not linked to Google
            BasicUserInfoApiDto existingUser = user.get();
            return new GoogleLoginDTO(
                    GoogleLoginDTO.LoginType.POSSIBLE_LINK,
                    "google",
                    googleUserDto.sub(),
                    existingUser.userId()
            );
        } else {
            // User does not exist, create a new OAuth user
            return registerViaGoogleUseCase.execute(googleUserDto);
        }
    }
}
