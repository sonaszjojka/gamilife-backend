package pl.gamilife.user.usecase.levelupuser;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class LevelUpUserUseCaseImpl implements LevelUpUserUseCase {

    private final UserRepository userRepository;

    @Override
    public Void execute(LevelUpUserCommand cmd) {
        User user = getUser(cmd.userId());

        user.levelUp(cmd.level());
        userRepository.save(user);

        return null;
    }

    private User getUser(UUID userId) {
        return userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }
}
