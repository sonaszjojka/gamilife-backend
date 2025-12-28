package pl.gamilife.grouptask.usecase.editgrouptask;

import pl.gamilife.grouptask.entity.GroupTask;

public interface EditGroupTaskMapper {
    EditGroupTaskResponse toResponse(GroupTask groupTask);
}
