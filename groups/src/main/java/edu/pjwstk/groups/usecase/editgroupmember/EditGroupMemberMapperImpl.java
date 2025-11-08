package edu.pjwstk.groups.usecase.editgroupmember;

import edu.pjwstk.groups.model.GroupMember;
import org.springframework.stereotype.Component;

@Component
public class EditGroupMemberMapperImpl implements EditGroupMemberMapper {

    @Override
    public EditGroupMemberResponse toResponse(GroupMember savedGroupMember) {
        if (savedGroupMember == null) {
            return null;
        }

        return EditGroupMemberResponse.builder()
                .groupMemberId(savedGroupMember.getGroupMemberId())
                .memberGroup(
                        savedGroupMember.getMemberGroup() != null
                                ? EditGroupMemberResponse.GroupDto.builder()
                                .groupId(savedGroupMember.getMemberGroup().getGroupId())
                                .adminId(savedGroupMember.getMemberGroup().getAdminId())
                                .build()
                                : null
                )
                .userId(savedGroupMember.getUserId())
                .joinedAt(savedGroupMember.getJoinedAt())
                .leftAt(savedGroupMember.getLeftAt())
                .groupMoney(savedGroupMember.getGroupMoney())
                .totalEarnedMoney(savedGroupMember.getTotalEarnedMoney())
                .build();
    }
}
