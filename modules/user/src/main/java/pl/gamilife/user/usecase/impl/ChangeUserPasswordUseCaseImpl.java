package pl.gamilife.user.usecase.impl;

import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.auth.AuthApi;
import pl.gamilife.api.auth.dto.AuthTokens;
import pl.gamilife.api.auth.dto.ChangePasswordDto;
import pl.gamilife.api.auth.dto.RotateUserTokensDto;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.dto.service.ChangeUserPasswordCommand;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.ChangeUserPasswordUseCase;

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
