package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.usecase.result.GoogleLoginResult;
import edu.pjwstk.auth.usecase.result.LoginUserResult;
import edu.pjwstk.auth.usecase.LoginViaGoogleUseCase;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.SecureUserInfoApiDto;
import edu.pjwstk.api.user.exception.UserNotFoundException;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class LoginViaGoogleUseCaseImpl implements LoginViaGoogleUseCase {

    private final UserApi userApi;
    private final TokenService tokenService;

    @Override
    public GoogleLoginResult execute(UUID userId, String googleEmail) {
        // User already exists with this Google provider ID
        SecureUserInfoApiDto user = userApi.getSecureUserDataById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        // Update email only if the user has no local account set up and the email is different
        if (user.password() == null && !user.email().equals(googleEmail)) {
            userApi.updateUserEmail(user.userId(), googleEmail);
        }

        return new GoogleLoginResult(
                GoogleLoginResult.LoginType.EXISTING_USER,
                new LoginUserResult(
                        user.userId(),
                        user.email(),
                        user.username(),
                        true,
                        tokenService.generateTokenPair(user.userId(), user.email(), true)
                )
        );
    }
}
