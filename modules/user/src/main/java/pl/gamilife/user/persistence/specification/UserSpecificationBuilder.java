package pl.gamilife.user.persistence.specification;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import pl.gamilife.user.persistence.UserEntity;

@Component
public class UserSpecificationBuilder {

    public Specification<UserEntity> build(String username) {
        return Specification.allOf(
                hasUsername(username),
                isEmailVerified()
        );
    }

    private Specification<UserEntity> hasUsername(String username) {
        return (root, query, cb) -> {
            if (username == null || username.isBlank()) {
                return null;
            }
            String pattern = "%" + username.trim().toLowerCase() + "%";
            return cb.like(cb.lower(root.get("username")), pattern);
        };
    }

    private Specification<UserEntity> isEmailVerified() {
        return (root, query, cb) ->
                cb.isTrue(root.get("isEmailVerified"));
    }
}

