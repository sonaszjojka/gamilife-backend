package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.usecase.result.GoogleLoginResult;
import edu.pjwstk.auth.dto.service.GoogleUserDto;
import edu.pjwstk.auth.usecase.result.LoginUserResult;
import edu.pjwstk.auth.models.UserOAuthProvider;
import edu.pjwstk.auth.repository.JpaUserProviderRepository;
import edu.pjwstk.auth.usecase.RegisterViaGoogleUseCase;
import edu.pjwstk.auth.service.TokenService;
import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.api.user.dto.RegisterUserApiDto;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

@Service
@AllArgsConstructor
public class RegisterViaGoogleUseCaseImpl implements RegisterViaGoogleUseCase {

    private final JpaUserProviderRepository userProviderRepository;
    private final UserApi userApi;
    private final TokenService tokenService;

    @Override
    @Transactional
    public GoogleLoginResult execute(GoogleUserDto googleUserDto) {
        String username = googleUserDto.firstName().substring(0, 3).toLowerCase() + "_" +
                googleUserDto.lastName().substring(0, 3).toLowerCase() + "_" +
                ThreadLocalRandom.current().nextInt(1000, 9999);

        RegisterUserApiDto newGoogleUser = new RegisterUserApiDto(
                googleUserDto.firstName(),
                googleUserDto.lastName(),
                googleUserDto.email(),
                null,
                username,
                null, // No birthdate provided
                false, // Default to not sending budget reports
                false, // Default to private profile
                true
        );
        BasicUserInfoApiDto createdGoogleUser = userApi.registerNewUser(newGoogleUser);

        userProviderRepository.save(new UserOAuthProvider(
                UUID.randomUUID(),
                createdGoogleUser.userId(),
                "google",
                googleUserDto.sub()
        ));


        return new GoogleLoginResult(
                GoogleLoginResult.LoginType.NEW_USER,
                new LoginUserResult(
                        createdGoogleUser.userId(),
                        createdGoogleUser.email(),
                        createdGoogleUser.username(),
                        true,
                        tokenService.generateTokenPair(
                                createdGoogleUser.userId(),
                                createdGoogleUser.email(),
                                true
                        )
                )
        );
    }
}
