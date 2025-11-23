package edu.pjwstk.groups.util;


import edu.pjwstk.groups.enums.GroupRequestStatusEnum;
import edu.pjwstk.groups.model.GroupRequest;
import org.springframework.data.jpa.domain.Specification;

import java.util.UUID;

public interface GroupRequestSpecificationBuilder {
    Specification<GroupRequest> buildSpecification(UUID groupId, GroupRequestStatusEnum statusEnum);
}