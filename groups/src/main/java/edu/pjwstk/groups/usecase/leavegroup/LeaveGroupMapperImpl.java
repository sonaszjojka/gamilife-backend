package edu.pjwstk.groups.usecase.leavegroup;

import edu.pjwstk.groups.entity.GroupMember;
import org.springframework.stereotype.Component;

@Component
public class LeaveGroupMapperImpl implements LeaveGroupMapper {
    @Override
    public LeaveGroupResponse toResponse(GroupMember savedGroupMember) {
        if (savedGroupMember == null) {
            return null;
        }

        return LeaveGroupResponse.builder()
                .groupMemberId(savedGroupMember.getGroupMemberId())
                .memberGroup(
                        savedGroupMember.getMemberGroup() != null
                                ? LeaveGroupResponse.GroupDto.builder()
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
