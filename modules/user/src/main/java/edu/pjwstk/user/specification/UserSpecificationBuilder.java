package edu.pjwstk.user.specification;

import edu.pjwstk.user.persistence.UserEntity;
import org.springframework.data.jpa.domain.Specification;

public interface UserSpecificationBuilder {
    Specification<UserEntity> buildSpecification(String username);
}
