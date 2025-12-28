package pl.gamilife.grouptask.repository;


import pl.gamilife.grouptask.entity.GroupTaskMember;

import java.util.Optional;
import java.util.UUID;

public interface GroupTaskMemberRepository {
    GroupTaskMember save(GroupTaskMember groupTaskMember);

    void deleteById(UUID groupTaskMemberId);

    boolean existsById(UUID groupTaskMemberId);

    Optional<GroupTaskMember> findByGroupTaskMemberId(UUID groupTaskMemberId);
}
