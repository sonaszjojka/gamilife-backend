package edu.pjwstk.grouptasks.usecase.deletegrouptaskmember;

import edu.pjwstk.grouptasks.exception.GroupTaskMemberNotFoundException;
import edu.pjwstk.grouptasks.repository.GroupTaskMemberRepository;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
public class DeleteGroupTaskMemberUseCaseImpl implements DeleteGroupTaskMemberUseCase {
    private final GroupTaskMemberRepository groupTaskMemberRepository;

    public DeleteGroupTaskMemberUseCaseImpl(GroupTaskMemberRepository groupTaskMemberRepository) {
        this.groupTaskMemberRepository = groupTaskMemberRepository;
    }


    @Override
    @Transactional
    public void execute(UUID groupTaskMemberId) {
        if (!groupTaskMemberRepository.existsById(groupTaskMemberId)) {
            throw new GroupTaskMemberNotFoundException("Group Task Member with id:" + groupTaskMemberId + " does not exist");
        }

        groupTaskMemberRepository.deleteById(groupTaskMemberId);
    }

}