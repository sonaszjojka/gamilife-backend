package pl.gamilife.user.persistence;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import pl.gamilife.user.domain.User;

@NoArgsConstructor(access = AccessLevel.NONE)
public class UserMapper {
    public static UserEntity toEntity(User user) {
        return new UserEntity(
                user.getId(),
                user.getFirstName(),
                user.getLastName(),
                user.getEmail(),
                user.getPassword(),
                user.getUsername(),
                user.getDateOfBirth(),
                user.getLevel(),
                user.getExperience(),
                user.getMoney(),
                user.isSendBudgetReports(),
                user.isProfilePublic(),
                user.isEmailVerified(),
                user.isTutorialCompleted(),
                user.getTimezone(),
                user.getLastTimezoneChange()
        );
    }

    public static User toDomain(UserEntity userEntity) {
        return new User(
                userEntity.getId(),
                userEntity.getFirstName(),
                userEntity.getLastName(),
                userEntity.getEmail(),
                userEntity.getPassword(),
                userEntity.getUsername(),
                userEntity.getDateOfBirth(),
                userEntity.getLevel(),
                userEntity.getExperience(),
                userEntity.getMoney(),
                userEntity.isSendBudgetReports(),
                userEntity.isProfilePublic(),
                userEntity.isEmailVerified(),
                userEntity.isTutorialCompleted(),
                userEntity.getTimezone(),
                userEntity.getLastTimezoneChange()
        );
    }
}
