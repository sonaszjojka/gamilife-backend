package edu.pjwstk.auth.usecase.impl;

import edu.pjwstk.auth.domain.UserOAuthProvider;
import edu.pjwstk.auth.dto.service.GoogleLoginDTO;
import edu.pjwstk.auth.dto.service.GoogleUserDto;
import edu.pjwstk.auth.dto.service.LoginUserResult;
import edu.pjwstk.auth.persistence.repository.UserProviderRepository;
import edu.pjwstk.auth.usecase.RegisterViaGoogleUseCase;
import edu.pjwstk.auth.util.TokenProvider;
import edu.pjwstk.common.userApi.UserApi;
import edu.pjwstk.common.userApi.dto.BasicUserInfoApiDto;
import edu.pjwstk.common.userApi.dto.RegisterUserApiDto;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

@Service
@AllArgsConstructor
public class RegisterViaGoogleUseCaseImpl implements RegisterViaGoogleUseCase {

    private final UserProviderRepository userProviderRepository;
    private final UserApi userApi;
    private final TokenProvider tokenProvider;

    @Override
    @Transactional
    public GoogleLoginDTO execute(GoogleUserDto googleUserDto) {
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



        return new GoogleLoginDTO(
                GoogleLoginDTO.LoginType.NEW_USER,
                new LoginUserResult(
                        createdGoogleUser.userId(),
                        createdGoogleUser.email(),
                        createdGoogleUser.username(),
                        true,
                        tokenProvider.generateTokenPair(
                                createdGoogleUser.userId(),
                                createdGoogleUser.email(),
                                true
                        )
                )
        );
    }
}
