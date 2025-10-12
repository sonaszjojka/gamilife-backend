package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.dto.service.AuthTokens;
import edu.pjwstk.auth.dto.service.GoogleLoginDTO;
import edu.pjwstk.auth.usecase.LoginViaGoogleUseCase;
import edu.pjwstk.auth.util.TokenProvider;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.exception.UserNotFoundException;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class LoginViaGoogleUseCaseImpl implements LoginViaGoogleUseCase {

    private final UserApi userApi;
    private final TokenProvider tokenProvider;

    @Override
    public GoogleLoginDTO execute(UUID userId, String googleEmail) {
        // User already exists with this Google provider ID
        BasicUserInfoApiDto user = userApi.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // Update email only if the user has no local account set up and the email is different
        if (user.password() == null && !user.email().equals(googleEmail)) {
            userApi.updateUserEmail(user.userId(), googleEmail);
        }

        AuthTokens authTokens = tokenProvider.generateTokenPair(user.userId(), user.email());
        return new GoogleLoginDTO(
                GoogleLoginDTO.LoginType.EXISTING_USER,
                authTokens.accessToken(),
                authTokens.refreshToken()
        );
    }
}
