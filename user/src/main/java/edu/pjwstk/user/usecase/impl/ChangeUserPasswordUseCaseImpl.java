package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.auth.dto.ChangePasswordDto;
import edu.pjwstk.api.auth.dto.RotateUserTokensDto;
import edu.pjwstk.core.exception.common.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.dto.service.ChangeUserPasswordCommand;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.ChangeUserPasswordUseCase;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class ChangeUserPasswordUseCaseImpl implements ChangeUserPasswordUseCase {

    private final UserRepository userRepository;
    private final AuthApi authApi;

    @Override
    @Transactional
    public AuthTokens execute(ChangeUserPasswordCommand dto) {
        User user = userRepository.getUserById(dto.userId())
                .orElseThrow(() -> new UserNotFoundException("User not found"));

        String hashedNewPassword = authApi.handleChangePassword(
                new ChangePasswordDto(dto.oldPassword(), user.getPassword(), dto.newPassword())
        );
        user.setPassword(hashedNewPassword);

        userRepository.save(user);

        return authApi.rotateUserTokens(new RotateUserTokensDto(
                user.getId(),
                user.getEmail(),
                user.isEmailVerified()
        ));
    }

}
