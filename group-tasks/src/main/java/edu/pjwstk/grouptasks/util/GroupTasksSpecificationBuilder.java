package edu.pjwstk.grouptasks.util;

import edu.pjwstk.grouptasks.entity.GroupTask;
import org.springframework.data.jpa.domain.Specification;

import java.util.UUID;

public interface GroupTasksSpecificationBuilder {

    Specification<GroupTask> build(
            UUID groupId,
            Boolean isAccepted
    );

    Specification<GroupTask> isCompleted(Boolean isCompleted);
    Specification<GroupTask> currentGroup(UUID groupId);
}
