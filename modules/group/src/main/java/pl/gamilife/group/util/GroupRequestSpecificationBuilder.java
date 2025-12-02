package pl.gamilife.group.util;


import pl.gamilife.group.enums.GroupRequestStatusEnum;
import pl.gamilife.group.model.GroupRequest;
import org.springframework.data.jpa.domain.Specification;

import java.util.UUID;

public interface GroupRequestSpecificationBuilder {
    Specification<GroupRequest> buildSpecification(UUID groupId, GroupRequestStatusEnum statusEnum);
}