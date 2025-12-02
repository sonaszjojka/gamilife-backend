package edu.pjwstk.grouptasks.repository;


import edu.pjwstk.grouptasks.entity.GroupTaskMember;

import java.util.Optional;
import java.util.UUID;

public interface GroupTaskMemberRepository {
    GroupTaskMember save(GroupTaskMember groupTaskMember);
    void deleteById(UUID groupTaskMemberId);
    boolean existsById(UUID groupTaskMemberId);
    Optional<GroupTaskMember> findByGroupTaskMemberId(UUID groupTaskMemberId);
}
