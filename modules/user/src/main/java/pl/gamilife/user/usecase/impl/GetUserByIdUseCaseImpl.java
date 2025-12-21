package pl.gamilife.user.usecase.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;
import pl.gamilife.user.usecase.GetUserByIdUseCase;

import java.util.Optional;
import java.util.UUID;

@Service
@AllArgsConstructor
public class GetUserByIdUseCaseImpl implements GetUserByIdUseCase {

    private final UserRepository userRepository;

    @Override
    public Optional<BasicUserInfoDto> execute(UUID userId) {
        Optional<User> optionalUser = userRepository.getUserById(userId);

        if (optionalUser.isEmpty()) {
            return Optional.empty();
        }

        User user = optionalUser.get();

        return Optional.of(new BasicUserInfoDto(
                user.getId(),
                user.getEmail(),
                user.getUsername(),
                user.getLevel(),
                user.getExperience(),
                user.getMoney()
        ));
    }
}
