package edu.pjwstk.grouptasks.usecase.creategrouptaskmember;

import edu.pjwstk.grouptasks.entity.GroupTask;
import edu.pjwstk.grouptasks.entity.GroupTaskMember;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public class CreateGroupTaskMemberMapperImpl implements CreateGroupTaskMemberMapper {


    @Override
    public GroupTaskMember toEntity( GroupTask groupTask, int groupMemberId,UUID groupTaskMemberId) {
        return GroupTaskMember.builder()
                .groupTaskMemberId(groupTaskMemberId)
                .groupTaskId(groupTask)
                .groupMemberId(groupMemberId)
                .isMarkedDone(false)
                .build();
    }

    @Override
    public CreateGroupTaskMemberResponse toResponse(GroupTaskMember groupTaskMember) {
        return CreateGroupTaskMemberResponse.builder()
                .groupTaskMemberId(groupTaskMember.getGroupTaskMemberId())
                .groupTaskId(groupTaskMember.getGroupTaskId().getGroupTaskId())
                .groupMemberId(groupTaskMember.getGroupMemberId())
                .isMarkedDone(groupTaskMember.getIsMarkedDone())
                .build();
    }


}
