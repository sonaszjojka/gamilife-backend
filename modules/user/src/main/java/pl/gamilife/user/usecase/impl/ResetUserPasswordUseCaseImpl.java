package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.infrastructure.core.exception.common.domain.ResetPasswordGenericException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.ResetUserPasswordUseCase;

import java.util.UUID;

@Service
@AllArgsConstructor
public class ResetUserPasswordUseCaseImpl implements ResetUserPasswordUseCase {

    private UserRepository userRepository;

    @Override
    public void execute(UUID userId, String hashedNewPassword) {
        User user = userRepository.getUserById(userId)
                .orElseThrow(ResetPasswordGenericException::new);

        user.setPassword(hashedNewPassword);

        userRepository.save(user);
    }
}
