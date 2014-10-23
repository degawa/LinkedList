module class_DoublyList
    use class_DoublyLink
    implicit none
    private
    public :: DoublyList
    public :: DoublyListIterator

    !Doubly Linked List
    !List操作のためのcomponentとtype-bound procedureを定義
    type :: DoublyList
        private
        class(DoublyLink),pointer :: firstLink => null() !Listの先頭の要素
        class(DoublyLink),pointer :: lastLink  => null() !Listの末尾の要素
        class(DoublyLink),pointer :: currLink  => null() !Listの現在の要素
        integer :: numberOfItem = 0
    contains
        procedure,pass :: printList               !Listの全要素を表示
        procedure,pass :: print                   !printListから呼ばれる（要素が持つprint()を呼ぶだけ）
        
        procedure,pass :: addDoublyLinkToList     !Listの現在位置の後ろに新しい要素を追加
        procedure,pass :: addDoublyLinkToFirst    !Listの先頭に新しい要素を追加
        procedure,pass :: addDoublyLinkToLast     !Listの末尾に新しい要素を追加
        procedure,pass :: removeCurrentDoublyLink !Listの現在位置の要素を削除．currLinkは一つ前へ
        procedure,pass :: removeFirstDoublyLink   !Listの先頭要素を削除．currLinkは一つ後ろへ
        procedure,pass :: removeLastDoublyLink    !Listの末尾要素を削除．currLinkは一つ前へ
        procedure,pass :: getCurrentValue         !Listの現在位置の要素を取得
        procedure,pass :: getValueOf              !List内の先頭から指定した番号目の要素を取得
        procedure,pass :: getFirstValue           !Listの先頭の要素を取得
        procedure,pass :: getLastValue            !Listの末尾の要素を取得
        procedure,pass :: isEmpty                 !Listが空かを調査
        procedure,pass :: isCurrentLinkFirst      !【削除予定】currLinkが先頭かどうかを調査．hasPrevLinkと機能が重複．動作チェックが不十分
        procedure,pass :: isCurrentLinkLast       !【削除予定】currLinkが末尾かどうかを調査．hasNextLinkと機能が重複．動作チェックが不十分
        procedure,pass :: hasNextLink             !次に要素があるかを調査．isCurrnetLinkLastと機能が重複
        procedure,pass :: hasPrevLink             !前に要素があるかを調査．isCurrnetLinkFirstと機能が重複
        procedure,pass :: clearList               !Listを空にする（List内の全要素を削除）
        procedure,pass :: incrementSizeOfList     !List要素の個数に+1
        procedure,pass :: decrementSizeOfList     !List要素の個数に-1
        procedure,pass :: getSizeOfList           !List要素の個数を取得

        generic        :: add         => addDoublyLinkToList
        generic        :: addFirst    => addDoublyLinkToFirst
        generic        :: addLast     => addDoublyLinkToLast
        generic        :: remove      => removeCurrentDoublyLink
        generic        :: removeFirst => removeFirstDoublyLink
        generic        :: removeLast  => removeLastDoublyLink
        generic        :: get         => getCurrentValue
        generic        :: getFirst    => getFirstValue
        generic        :: getLast     => getLastValue
        generic        :: clear       => clearList
        generic        :: size        => getSizeOfList
        generic        :: isFirst     => isCurrentLinkFirst
        generic        :: isLast      => isCurrentLinkLast

        procedure,  pass :: iterator        !iteratorのコンストラクタ
        procedure,nopass :: destroyIterator !iteratorのデストラクタ
        
    end type DoublyList

    !Doubly Linked List走査用のiterator
    !JavaのListに準じて，hasNext,next, removeを定義
    type :: DoublyListIterator
        private
        type(DoublyList),pointer :: List => null()
        
        type(DoublyLink),pointer :: LastAccessedLink => null() !最後にアクセスしたListの要素へのポインタ．nextやremoveの実装に必要
        type(DoublyLink),pointer ::      iterateLink => null() !Listを走査するためのポインタ
    contains
        procedure,pass :: hasNext !Listに次の要素があるか
        procedure,pass :: next    !Listの現在の要素を返してcurrLinkを一つ次へ進める

        procedure,pass         :: moveToFirst             !IteratorをList先頭へ（Javaのようなガベッジコレクションがないため，iteratorを再利用する）
        procedure,pass         :: moveToLast              !【削除予定】IteratorをList末尾へ（使いどころが不明）
        procedure,pass         :: removeObtainedItem      !nextで取得した要素をListから削除
        procedure,pass,private :: removeObtainedFirstItem !removeObtainedItemから呼ばれる．nextで取得した要素が先頭の場合に利用
        procedure,pass,private :: removeObtainedLastItem  !removeObtainedItemから呼ばれる．nextで取得した要素が先頭の場合に利用
        
        generic        :: first       => moveToFirst
        generic        :: last        => moveToLast
        generic        :: remove      => removeObtainedItem
        generic        :: removeFirst => removeObtainedFirstItem
        generic        :: removeLast  => removeObtainedLastItem

    end type DoublyListIterator
    
    contains
    
!---------------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !Iterator用ヘルパ関数群
    !-----------------------------------------------------------------------------------------!
    !コンストラクタ
    function iterator(this)
        implicit none
        class(DoublyList),target :: this
        type(DoublyListIterator),pointer :: iterator
        
        allocate(DoublyListIterator::iterator)
        iterator%List => this
        call iterator%first()
    end function iterator
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !デストラクタ
    subroutine destroyIterator(iterator)
        implicit none
        type(DoublyListIterator),pointer :: iterator
        
        deallocate(iterator)

    end subroutine destroyIterator
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !Listに次の要素があるかを調査
    !do-whileの継続条件に利用
    function hasNext(this)
        implicit none
        class(DoublyListIterator) :: this
        logical hasNext
        
        if( associated(this%List%currLink) )then
            hasNext = .true.
        else
            hasNext = .false.
        end if

    end function hasNext
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !Listの現在の要素を返してcurrLinkを一つ次へ進める
    function next(this)
        implicit none
        class(DoublyListIterator) :: this
        class(*),pointer :: next

        if(associated(this%List%currLink))then
            this%LastAccessedLink => this%List%currLink
            next => this%List%currLink
            this%List%currLink=>this%List%currLink%getNextLink()
        else
            next => null()
        end if
        
    end function next
    !-----------------------------------------------------------------------------------------!

    !-----------------------------------------------------------------------------------------!
    !remove用関数群
    !-----------------------------------------------------------------------------------------!
    !nextで取得した要素をListから削除
    subroutine removeObtainedItem(this)
        implicit none
        class(DoublyListIterator) :: this
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%List%isEmpty() )then
            stop 'empty list'
        else
            if( .not.associated(this%LastAccessedLink%getNextLink()) )then !current is last
                !print *,"remove last items"
                call this%removeLast()
            else&
            if( .not.associated(this%LastAccessedLink%getPrevLink()) )then !current is first
                !print *,"remove first items"
                call this%removeFirst()
            else                                                   !elsewhere
                !print *,"remove items"
                prevLink => this%LastAccessedLink%getPrevLink()
                nextLink => this%LastAccessedLink%getNextLink()
                call prevLink%setNextLink(nextLink)
                call nextLink%setPrevLink(prevLink)
                deallocate(this%lastAccessedLink)
                this%List%currLink => nextLink
                prevLink => null()
                nextLink => null()
                this%LastAccessedLink => this%List%currLink%getPrevLink()
                call this%List%decrementSizeOfList()
                !print *,"insert"
            end if
        end if
        
    end subroutine removeObtainedItem
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !nextで取得した要素をListから削除
    !removeObtainedItemから呼ばれる．nextで取得した要素が先頭の場合に利用
    subroutine removeObtainedFirstItem(this)
        implicit none
        class(DoublyListIterator) :: this
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%List%isEmpty() )then
            stop 'empty list'
        else
            prevLink => this%LastAccessedLink%getPrevLink()
            nextLink => this%LastAccessedLink%getNextLink()

            call nextLink%setPrevLink(prevLink)
            deallocate(this%lastAccessedLink)
            this%List%currLink => nextLink
            this%List%firstLink => this%List%currLink
            prevLink => null()
            nextLink => null()
            this%LastAccessedLink => this%List%currLink
            call this%List%decrementSizeOfList()
        end if
        
    end subroutine removeObtainedFirstItem
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !nextで取得した要素をListから削除
    !removeObtainedItemから呼ばれる．nextで取得した要素が先頭の場合に利用
    subroutine removeObtainedLastItem(this)
        implicit none
        class(DoublyListIterator) :: this
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%List%isEmpty() )then
            stop 'empty list'
        else
            prevLink => this%LastAccessedLink%getPrevLink()
            nextLink => this%LastAccessedLink%getNextLink()
            call prevLink%setNextLink(nextLink)

            deallocate(this%lastAccessedLink)
            this%List%lastLink    => prevLink
            this%LastAccessedLink => prevLink
            this%iterateLink      => null()
            this%List%currLink    => null() !iteratorではnext()で一つ先へcurrentがずれる
            prevLink => null()
            nextLink => null()
            call this%List%decrementSizeOfList()
        end if
        
    end subroutine removeObtainedLastItem
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    subroutine moveToFirst(this)
        implicit none
        class(DoublyListIterator) :: this
        
        this%List%currLink => this%List%firstLink
    end subroutine moveToFirst
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    subroutine moveToLast(this)
        implicit none
        class(DoublyListIterator) :: this
        
        this%List%currLink => this%List%lastLink
    end subroutine moveToLast
    !-----------------------------------------------------------------------------------------!
    
!-------------------------doubly list用 type-bound procedure----------------------------------!
    !-----------------------------------------------------------------------------------------!
    !ヘルパ関数群
    !-----------------------------------------------------------------------------------------!
    !Listが空かを調査
    function isEmpty(this)
        implicit none
        class(DoublyList) :: this
        logical :: isEmpty
        
        if(associated(this%firstLink))then
            isEmpty = .false.
        else
            isEmpty = .true.
        end if
    end function isEmpty
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !【削除予定】currLinkが先頭かどうかを調査．hasPrevLinkと機能が重複．動作チェックが不十分
    function isCurrentLinkFirst(this) result(isFirst)
        use iso_c_binding
        implicit none
        
        class(DoublyList) :: this
        logical :: isFirst
        
        type(c_ptr) :: ptr_curr, ptr_first
        
        ptr_curr  = c_loc(this%currLink ); call c_f_pointer(ptr_curr, this%currLink)
        ptr_first = c_loc(this%firstLink); call c_f_pointer(ptr_first, this%firstLink)
        !print *,c_associated(ptr_curr,ptr_first)
        
        if(c_associated(ptr_curr,ptr_first))then
            isFirst = .true.
        else
            isFirst = .false.
        end if
        
    end function isCurrentLinkFirst
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !【削除予定】currLinkが末尾かどうかを調査．hasNextLinkと機能が重複．動作チェックが不十分
    function isCurrentLinkLast(this) result(isLast)
        use iso_c_binding
        implicit none
        
        class(DoublyList) :: this
        logical :: isLast
        
        type(c_ptr) :: ptr_curr, ptr_last
        
        ptr_curr = c_loc(this%currLink); call c_f_pointer(ptr_curr, this%currLink)
        ptr_last = c_loc(this%LastLink); call c_f_pointer(ptr_last, this%lastLink)
        !print *,c_associated(ptr_curr,ptr_first)
        
        if(c_associated(ptr_curr,ptr_last))then
            isLast = .true.
        else
            isLast = .false.
        end if
        
    end function isCurrentLinkLast
    !-----------------------------------------------------------------------------------------!
    !Listを走査するための関数
    !-----------------------------------------------------------------------------------------!
    !次に要素があるかを調査．isCurrnetLinkLastと機能が重複
    function hasNextLink(this)
        implicit none
        class(DoublyList) :: this
        logical :: hasNextLink
        
        if(associated(this%currLink%getNextLink()))then
            hasNextLink = .true.
        else
            hasNextLink = .false.
        end if
    end function hasNextLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !前に要素があるかを調査．isCurrnetLinkFirstと機能が重複
    function hasPrevLink(this)
        implicit none
        class(DoublyList) :: this
        logical :: hasPrevLink
        
        if(associated(this%currLink%getPrevLink()))then
            hasPrevLink = .true.
        else
            hasPrevLink = .false.
        end if
    end function hasPrevLink
    !-----------------------------------------------------------------------------------------!
    
    !-----------------------------------------------------------------------------------------!
    !画面表示用関数．動作確認用に作成．実用上特に意義が感じられないので削除を推奨
    !-----------------------------------------------------------------------------------------!
    !Listの全要素を表示
    subroutine print(this)
        class(DoublyList) :: this
        
        if(.not.this%isEmpty())then
            call this%currLink%print()
        end if
    end subroutine print
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !printListから呼ばれる（要素が持つprint()を呼ぶだけ）
    subroutine printList(this)
        class(Doublylist) :: this
        class(DoublyLink),pointer :: doublyLinkItem
        !class(*),pointer :: item !select type用

        type(DoublyListIterator),pointer :: itr
    
        if(this%isEmpty())then
            print *,"empty list"
            return
        end if
        
        itr => this%iterator()
        do while(itr%hasNext())
            !next()はclass(*)を返すのでselect typeで適切な型（doublyLink）を選ぶ必要がある
            !item=>itr%next()
            !select type(item)
            !class is(DoublyLink)
            !    doublyLinkItem => item
            !    call doublyLinkItem%print()
            !end select
            !もしくは擬似的にキャストを行う関数を使う
            doublyLinkItem => toDoublyLink(itr%next())
            call doublyLinkItem%print()
        end do
        call this%destroyIterator(itr)
        
    end subroutine printList
    !-----------------------------------------------------------------------------------------!
    !Listの要素数
    !-----------------------------------------------------------------------------------------!
    !List要素数を+1
    subroutine incrementSizeOfList(this)
        class(Doublylist) :: this
        
        this%numberOfItem = this%numberOfItem + 1
    end subroutine incrementSizeOfList
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List要素数を-1
    subroutine decrementSizeOfList(this)
        class(Doublylist) :: this
        
        this%numberOfItem = this%numberOfItem - 1
    end subroutine decrementSizeOfList
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List要素数を取得
    integer function getSizeOfList(this)
        class(Doublylist) :: this
        
        getSizeOfList = this%numberOfItem
    end function getSizeOfList
    !-----------------------------------------------------------------------------------------!

    !-----------------------------------------------------------------------------------------!
    !Listへ要素を追加
    !-----------------------------------------------------------------------------------------!
    !Listの現在位置の後ろに新しい要素を追加
    subroutine addDoublyLinkToList(this,value)
        implicit none
        class(DoublyList),intent(inout) :: this
        class(*),intent(in)    :: value
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%isEmpty() )then                      !Listが空
            call this%addFirst(value)
            !print *,"create link and add to first"
        else if( .not.associated(this%currLink) )then !カレント要素がNullの時は終端と判断
            call this%addLast(value)
        else                                          !Listが空でなければ
            !前と後ろが空いていれば，後ろに追加
            if( .not.this%hasNextLink() )then         !currentがLastなら末尾に追加
                !print *,"add to last"
                call this%addLast(value)
            else
                !print *,"insert"
                prevLink => this%currLink             !追加するリンクにとって前要素とはカレント要素
                nextLink => this%currLink%getNextLink()
                this%currLink => new_DoublyLink(value, prevLink, nextLink)
                call prevLink%setNextLink(this%currLink)
                call nextLink%setPrevLink(this%currLink)
                prevLink => null()
                nextLink => null()
                if( .not.associated(this%currLink%getNextLink()) ) this%lastLink => this%currLink
                call this%incrementSizeOfList()
            end if
        end if

    end subroutine addDoublyLinkToList
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !Listの先頭に新しい要素を追加
    subroutine addDoublyLinkToFirst(this,value)
        implicit none
        class(DoublyList),intent(inout) :: this
        class(*),intent(in)    :: value
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
            prevLink => null()
        if( this%isEmpty() )then !Listが空
            nextLink => null()
            this%firstLink => new_DoublyLink(value, prevLink, nextLink)
            this% lastLink => this%firstLink
        else
            nextLink => this%firstLink
            this%firstLink => new_DoublyLink(value, prevLink, nextLink)
            nextLink => this%firstLink%getNextLink()
            call nextLink%setPrevLink(this%firstLink)
            nextLink => null()
        end if
            nextLink => null()
            prevLink => null()
        
        this% currLink => this%firstLink !if necessary
        call this%incrementSizeOfList()

    end subroutine addDoublyLinkToFirst
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !Listの末尾に新しい要素を追加
    subroutine addDoublyLinkToLast(this,value)
        implicit none
        class(DoublyList),intent(inout) :: this
        class(*),intent(in)    :: value
        
        class(DoublyLink),pointer ::  newLink
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%isEmpty() )then !Listが空
            call this%addFirst(value)
        else
            prevLink => this%lastLink
            nextLink => null()
             newLink => new_DoublyLink(value, prevLink, nextLink)
            prevLink => newLink%getPrevLink()
            call prevLink%setNextLink(newLink)
            this%lastLink => newLink
            call this%incrementSizeOfList()
        end if
            this%currLink => this%LastLink !if necessary
            nextLink => null()
            prevLink => null()
             newLink => null()
    
    end subroutine addDoublyLinkToLast
    !-----------------------------------------------------------------------------------------!
    
    !-----------------------------------------------------------------------------------------!
    !Listから要素を削除
    !known bug これらの関数でListを空にするとアクセス違反が発生．Listを空にするときはclearを使用
    !-----------------------------------------------------------------------------------------!
    !Listの現在位置の要素を削除．currLinkは一つ前へ
    subroutine removeCurrentDoublyLink(this)
        implicit none
        class(DoublyList),intent(inout) :: this
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%isEmpty() )then
            stop 'empty list'
        else
            if( .not.this%hasNextLink() )then !currentが末尾なら
                call this%removeLast()
            else&
            if( .not.this%hasPrevLink() )then !currentが先頭なら
                call this%removeFirst()
            else                              !それ以外
                prevLink => this%currLink%getPrevLink()
                nextLink => this%currLink%getNextLink()
                deallocate(this%currLink)
                this%currLink => prevLink
                call this%currLink%setNextLink(nextLink)
                call      nextLink%setPrevLink(this%currLink)
                prevLink => null()
                nextLink => null()

                call this%decrementSizeOfList()
            end if
        end if
        
    end subroutine removeCurrentDoublyLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !Listの先頭要素を削除．currLinkは一つ後ろへ
    subroutine removeFirstDoublyLink(this)
        implicit none
        class(DoublyList),intent(inout) :: this
        
        class(DoublyLink),pointer :: prevLink
        class(DoublyLink),pointer :: nextLink
        
        if( this%isEmpty() )then
            stop 'emtpy list'
        else
            this%currLink => this%firstLink
            nextLink => this%currLink%getNextLink()
            deallocate(this%currLink)
            this%currLink => nextLink
            prevLink => null()
            call this%currLink%setPrevLink(prevLink)
            this%firstLink => this%currLink
        end if
        prevLink => null()
        nextLink => null()
        
        call this%decrementSizeOfList()
        
    end subroutine removeFirstDoublyLink
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !Listの末尾要素を削除．currLinkは一つ前へ
    subroutine removeLastDoublyLink(this)
        implicit none
        class(DoublyList),intent(inout) :: this
        
        class(DoublyLink),pointer :: nextLink
        class(DoublyLink),pointer :: prevLink
        
        if( this%isEmpty() )then
            stop 'empty list'
        else
            this%currLink => this%lastLink
            prevLink => this%lastLink%getPrevLink()
            deallocate(this%lastLink)
            this%lastLink => prevLink
            nextLink      => null()
            call this%lastLink%setNextLink(nextLink)
            this%currLink => this%LastLink
        end if
        
        call this%decrementSizeOfList()
        
    end subroutine removeLastDoublyLink
    !-----------------------------------------------------------------------------------------!

    !-----------------------------------------------------------------------------------------!
    !Listを空にする（List内の全要素を削除）
    subroutine clearList(this)
        implicit none
        class(DoublyList),intent(inout) :: this
        
        class(DoublyLink),pointer :: nextLink
        
        if( this%isEmpty() )then
            !Listが空なら何もしない
        else
            this%currLink => this%firstLink
            do while(associated(this%currLink))
                nextLink=>this%currLink%getNextLink()
                deallocate(this%currLink)
                this%currLink  => nextLink
            end do
        end if
        this%firstLink => null()
        this%lastLink  => null()
             nextLink  => null()
        
        this%numberOfItem = 0
        
    end subroutine clearList
    !-----------------------------------------------------------------------------------------!
    
    !-----------------------------------------------------------------------------------------!
    !Listの要素を取得
    !-----------------------------------------------------------------------------------------!
    !Listの現在位置の要素を取得
    function getCurrentValue(this) result(value)
        implicit none
        class(DoublyList) :: this
        
        class(*),pointer :: value
        
        if(.not.this%isEmpty())then
            value=>this%currLink%getValue()
        end if
    end function getCurrentValue
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !List内の先頭から指定した番号目の要素を取得．O(N)の時間が必要
    function getValueOf(this,index) result(value)
        implicit none
        class(DoublyList) :: this
        integer,intent(in),value :: index
        integer :: i
        
        class(*),pointer :: value
        
        value => null()
        if(index > this%numberOfItem .or. this%isEmpty()) stop "index error"
        
        i=1
        this%currLink => this%firstLink
        do i=1,index-1
            this%currLink => this%currLink%getNextLink()
        end do
            value=>this%currLink%getValue()
            
    end function getValueOf
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !Listの先頭の要素を取得
    function getFirstValue(this) result(value)
        implicit none
        class(DoublyList) :: this
        
        class(*),pointer :: value
        
        if(.not.this%isEmpty())then
            value=>this%firstLink%getValue()
        end if
    end function getFirstValue
    !-----------------------------------------------------------------------------------------!
    !-----------------------------------------------------------------------------------------!
    !Listの末尾の要素を取得
    function getLastValue(this) result(value)
        implicit none
        class(DoublyList) :: this
        
        class(*),pointer :: value
        
        if(.not.this%isEmpty())then
            value=>this%lastLink%getValue()
        end if
    end function getLastValue
    !-----------------------------------------------------------------------------------------!

end module class_DoublyList