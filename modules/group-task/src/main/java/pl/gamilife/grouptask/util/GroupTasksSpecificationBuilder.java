package pl.gamilife.grouptask.util;

import pl.gamilife.grouptask.entity.GroupTask;
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
