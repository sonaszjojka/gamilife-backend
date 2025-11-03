package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.api.auth.AuthApi;
import edu.pjwstk.api.auth.dto.AuthTokens;
import edu.pjwstk.api.auth.dto.ChangePasswordCommand;
import edu.pjwstk.api.auth.dto.RotateUserTokensCommand;
import edu.pjwstk.api.user.exception.UserNotFoundException;
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
                new ChangePasswordCommand(dto.oldPassword(), user.getPassword(), dto.newPassword())
        );
        user.setPassword(hashedNewPassword);

        userRepository.save(user);

        return authApi.rotateUserTokens(new RotateUserTokensCommand(
                user.getId(),
                user.getEmail(),
                user.isEmailVerified()
        ));
    }

}
