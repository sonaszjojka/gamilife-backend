package edu.pjwstk.grouptasks.usecase.editgrouptask;

import edu.pjwstk.grouptasks.entity.GroupTask;

public interface EditGroupTaskMapper {
    EditGroupTaskResponse toResponse(GroupTask groupTask);
}
