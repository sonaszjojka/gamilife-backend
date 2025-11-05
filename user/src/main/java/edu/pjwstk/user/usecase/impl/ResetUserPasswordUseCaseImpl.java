package edu.pjwstk.user.usecase.impl;

import edu.pjwstk.core.exception.common.ResetPasswordGenericException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import edu.pjwstk.user.usecase.ResetUserPasswordUseCase;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

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
