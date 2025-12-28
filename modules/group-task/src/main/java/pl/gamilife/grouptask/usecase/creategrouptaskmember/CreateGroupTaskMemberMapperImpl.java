package pl.gamilife.grouptask.usecase.creategrouptaskmember;

import org.springframework.stereotype.Component;
import pl.gamilife.grouptask.entity.GroupTask;
import pl.gamilife.grouptask.entity.GroupTaskMember;

import java.util.UUID;

@Component
public class CreateGroupTaskMemberMapperImpl implements CreateGroupTaskMemberMapper {


    @Override
    public GroupTaskMember toEntity(GroupTask groupTask, UUID groupMemberId) {
        return GroupTaskMember.create(groupTask, groupMemberId);
    }

    @Override
    public CreateGroupTaskMemberResponse toResponse(GroupTaskMember groupTaskMember) {
        return CreateGroupTaskMemberResponse.builder()
                .groupTaskMemberId(groupTaskMember.getId())
                .groupTaskId(groupTaskMember.getGroupTaskId())
                .groupMemberId(groupTaskMember.getGroupMemberId())
                .isMarkedDone(groupTaskMember.isMarkedDone())
                .build();
    }


}
