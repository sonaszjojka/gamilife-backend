package edu.pjwstk.user.usecase.levelupuser;

import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class LevelUpUserUseCaseImpl implements LevelUpUserUseCase {

    private final UserRepository userRepository;

    @Override
    public Void execute(LevelUpUserCommand cmd) {
        User user = getUser(cmd.userId());

        user.setLevel(cmd.level());
        userRepository.save(user);

        return null;
    }

    private User getUser(UUID userId) {
        return userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }
}
