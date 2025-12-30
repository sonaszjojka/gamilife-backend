package pl.gamilife.group.controller;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.group.controller.request.CreateChatMessageRequest;
import pl.gamilife.group.usecase.createchatmessage.CreateChatMessageCommand;
import pl.gamilife.group.usecase.createchatmessage.CreateChatMessageResult;
import pl.gamilife.group.usecase.createchatmessage.CreateChatMessageUseCase;
import pl.gamilife.group.usecase.getchatmessages.GetChatMessagesCommand;
import pl.gamilife.group.usecase.getchatmessages.GetChatMessagesResult;
import pl.gamilife.group.usecase.getchatmessages.GetChatMessagesUseCase;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/groups/{groupId}/members/{groupMemberId}/chat-messages")
public class ChatMessageController {

    private final CreateChatMessageUseCase createChatMessageUseCase;
    private final GetChatMessagesUseCase getChatMessagesUseCase;


    @PostMapping
    public ResponseEntity<CreateChatMessageResult> save(
            @CurrentUserId UUID userId,
            @RequestBody @Valid CreateChatMessageRequest request,
            @PathVariable UUID groupId,
            @PathVariable UUID groupMemberId
    ) {
        CreateChatMessageResult response = createChatMessageUseCase.execute(new CreateChatMessageCommand(
                userId,
                groupId,
                groupMemberId,
                request.content(),
                request.isImportant()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @GetMapping
    public ResponseEntity<GetChatMessagesResult> getAllChatMessages(
            @PathVariable("groupId") UUID groupId,
            @RequestParam(required = false) Boolean isImportant,
            @RequestParam(defaultValue = "0") @Min(0) Integer page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(100) Integer size) {

        GetChatMessagesResult response = getChatMessagesUseCase.execute(
                new GetChatMessagesCommand(
                        groupId,
                        isImportant,
                        page,
                        size
                )
        );
        return ResponseEntity.ok(response);
    }
}