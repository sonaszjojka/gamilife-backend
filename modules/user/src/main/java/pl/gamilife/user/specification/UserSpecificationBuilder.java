package pl.gamilife.user.specification;

import pl.gamilife.user.persistence.UserEntity;
import org.springframework.data.jpa.domain.Specification;

public interface UserSpecificationBuilder {
    Specification<UserEntity> buildSpecification(String username);
}
