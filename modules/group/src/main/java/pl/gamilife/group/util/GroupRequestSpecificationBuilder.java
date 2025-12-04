package pl.gamilife.group.util;


import org.springframework.data.jpa.domain.Specification;
import pl.gamilife.group.enums.GroupRequestStatusEnum;
import pl.gamilife.group.model.GroupRequest;

import java.util.UUID;

public interface GroupRequestSpecificationBuilder {
    Specification<GroupRequest> buildSpecification(UUID groupId, GroupRequestStatusEnum statusEnum);
}