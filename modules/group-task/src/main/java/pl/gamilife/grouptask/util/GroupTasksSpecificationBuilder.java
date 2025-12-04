package pl.gamilife.grouptask.util;

import org.springframework.data.jpa.domain.Specification;
import pl.gamilife.grouptask.entity.GroupTask;

import java.util.UUID;

public interface GroupTasksSpecificationBuilder {

    Specification<GroupTask> build(
            UUID groupId,
            Boolean isAccepted
    );

    Specification<GroupTask> isCompleted(Boolean isCompleted);

    Specification<GroupTask> currentGroup(UUID groupId);
}
