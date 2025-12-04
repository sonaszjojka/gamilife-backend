package pl.gamilife.user.specification;

import org.springframework.data.jpa.domain.Specification;
import pl.gamilife.user.persistence.UserEntity;

public interface UserSpecificationBuilder {
    Specification<UserEntity> buildSpecification(String username);
}
