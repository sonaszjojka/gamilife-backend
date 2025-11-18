package edu.pjwstk.groups.usecase.getchatmessages;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.BasicUserInfoApiDto;
import edu.pjwstk.groups.model.ChatMessage;
import edu.pjwstk.groups.repository.ChatMessageJpaRepository;
import edu.pjwstk.groups.util.ChatMessageSpecificationBuilder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class GetChatMessagesUseCaseImpl implements GetChatMessagesUseCase {

    private final ChatMessageJpaRepository chatMessageRepository;
    private final ChatMessageSpecificationBuilder specificationBuilder;
    private final UserApi userApi;

    @Override
    @Transactional(readOnly = true)
    public GetChatMessagesResult executeInternal(GetChatMessagesCommand cmd) {
        log.debug("Fetching chat messages with filters: {}", cmd);

        Page<ChatMessage> messagePage = chatMessageRepository.findAll(
                getChatMessageSpecification(cmd),
                createPageable(cmd)
        );

        List<UUID> messageIds = messagePage.map(ChatMessage::getMessageId).getContent();

        List<ChatMessage> messagesWithDetails;
        if (!messageIds.isEmpty()) {
            messagesWithDetails = chatMessageRepository.findWithGroupMemberByMessageIdIn(messageIds);
            messagesWithDetails.sort(Comparator.comparingInt(m -> messageIds.indexOf(m.getMessageId())));
        } else {
            messagesWithDetails = List.of();
        }

        log.debug("Found {} chat messages", messagePage.getTotalElements());

        return buildGetChatMessagesResult(messagePage, messagesWithDetails);
    }

    private Specification<ChatMessage> getChatMessageSpecification(GetChatMessagesCommand cmd) {
        return specificationBuilder.buildSpecification(
                cmd.groupId(),
                cmd.isImportant()
        );
    }

    private Pageable createPageable(GetChatMessagesCommand cmd) {
        return PageRequest.of(
                cmd.page(),
                cmd.size(),
                Sort.by(Sort.Direction.DESC, "sentAt")
        );
    }

    private GetChatMessagesResult buildGetChatMessagesResult(Page<ChatMessage> messagePage, List<ChatMessage> messages) {
        return new GetChatMessagesResult(
                messagePage.getTotalPages(),
                messagePage.getTotalPages(),
                messagePage.getNumber(),
                messagePage.getSize(),
                messages.stream().map(m -> new GetChatMessagesResult.ChatMessageDto(
                        m.getMessageId(),
                        m.getContent(),
                        m.getSentAt(),
                        m.getIsImportant(),
                        m.getGroupId(),
                        m.getGroupMemberId(),
                        getSenderUsername(m.getGroupMember().getUserId())
                )).toList()
        );
    }

    private String getSenderUsername(UUID userId) {
        return userApi.getUserById(userId)
                .map(BasicUserInfoApiDto::username)
                .orElse(null);
    }
}